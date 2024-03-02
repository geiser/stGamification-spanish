const uuid = require("uuid");
const fs = require("fs");
const config = require("./src/config.json");

var sessionData = {};
/*
{
    session: { sessionId, ip, environmentName, created, redirectedToPretest, returnedFromPretest, redirectedToPosttest, active }
    redirectionToPretest: { sessionId, url, params, timestamp }
    returnFromPretest: { sessionId, headers, body, timestamp }
    redirectionToPosttest: { sessionId, url, params, timestamp }
    answers: { sessionId, questionId, answer }
    trophies: { sessionId, trophyId }
    game: { environmentName, started, ended }
}
*/

try {
    sessionData = JSON.parse(fs.readFileSync("./sessions.json").toString("utf-8"));
} catch (e) {
    if (e.code !== "ENOENT") {
        throw e;
    }
}

function selectEnvironment() {
    const participantAllocation = config.participantAllocation;
    const environments = Object.keys(config.environments);

    if (participantAllocation === "random") {
        return rand(environments);
    } else if (!isNaN(participantAllocation)) {
        let noParticipants = Object.keys(sessionData).length;

        return environments[Math.trunc(noParticipants / participantAllocation) % environments.length];
    } else {
        throw new Error("invalid environment");
    }
}

function getSessionData(sessionId) {
    return sessionData[sessionId] || {};
}

function sessionExists(sessionId) {
    return sessionData.hasOwnProperty(sessionId);
}

function getSession(sessionId) {
    return new Promise((resolve, reject) => {
        if (!sessionExists(sessionId))
            return reject("session does not exist");
        
        const session = getSessionData(sessionId).session;

        return resolve({
            sessionId,
            environmentName: session.environmentName,
            created: session.created,
            redirectedToPretest: session.redirectedToPretest,
            returnedFromPretest: session.returnedFromPretest,
            redirectedToPosttest: session.redirectedToPosttest,
            active: session.active,
        })
    });
}

function createSession(ip) {
    return new Promise((resolve, reject) => {
        const sessionId = uuid.v4();

        const session = {
            sessionId,
            ip,
            environmentName: selectEnvironment(),
            created: Date.now(),
            redirectedToPretest: false,
            returnedFromPretest: false,
            redirectedToPosttest: false,
            active: true,
        };

        sessionData[sessionId] = {
            session,
            redirectionToPretest: {},
            returnFromPretest: {},
            redirectionToPosttest: {},
            answers: {},
            trophies: {},
            game: {},
        };

        resolve({
            sessionId: sessionId,
            environmentName: session.environmentName,
            created: session.created,
            redirectedToPretest: false,
            returnedFromPretest: false,
            redirectedToPosttest: false,
        });
    })
}

function closeSession(sessionId) {
    return new Promise((resolve, reject) => {
        if (!sessionExists(sessionId)) {
            return reject("invalid session");
        }

        sessionData[sessionId].session.active = false;

        resolve();
    });
}

function setUserSentToPretest(sessionId, url) {
    return new Promise((resolve, reject) => {
        if (!sessionExists(sessionId)) {
            return reject("invalid session");
        } else if (!sessionId || !url) {
            return reject("invalid params");
        } else if (sessionData[sessionId].session.redirectedToPretest) {
            // was already redirected
            return resolve(sessionData[sessionId].redirectionToPretest);
        }

        sessionData[sessionId].session.redirectedToPretest = true;
        sessionData[sessionId].redirectionToPretest = {
            sessionId,
            url,
            timestamp: Date.now(),
            params: Object.fromEntries(new URLSearchParams(url.substr(url.indexOf("?")))),
        };

        resolve(sessionData[sessionId].redirectionToPretest);
    });
}

function setReturnFromPretest(sessionId, referrer, url) {
    return new Promise((resolve, reject) => {
        if (!sessionExists(sessionId)) {
            return reject("invalid session");
        } else if (!sessionId || !referrer || !url) {
            return reject("invalid params");
        } else if (sessionData[sessionId].session.returnedFromPretest) {
            // already returned
            return resolve(sessionData[sessionId].returnFromPretest);
        }

        sessionData[sessionId].session.returnedFromPretest = true;
        sessionData[sessionId].returnFromPretest = {
            sessionId,
            referrer,
            url,
            params: Object.fromEntries(new URLSearchParams(url.substr(url.indexOf("?")))),
            timestamp: Date.now(),
        };

        resolve(sessionData[sessionId].returnFromPretest);
    });
}

function setReturnFromPretestHeaders(sessionId, headers) {
    return new Promise((resolve, reject) => {
        if (!sessionExists(sessionId)) {
            return reject("invalid session");
        } else if (!sessionId || !headers) {
            return reject("invalid params");
        } else if (sessionData[sessionId].hasOwnProperty("returnFromPretestHeaders")) {
            return resolve();
        }

        sessionData[sessionId].returnFromPretestHeaders = headers;

        resolve();
    });
}

function setUserSentToPosttest(sessionId, url) {
    return new Promise((resolve, reject) => {
        if (!sessionExists(sessionId)) {
            return reject("invalid session");
        } else if (!sessionId || !url) {
            return reject("invalid params");
        } else if (sessionData[sessionId].session.redirectedToPosttest) {
            // was already redirected
            return resolve(sessionData[sessionId].redirectionToPosttest);
        }
    
        sessionData[sessionId].session.redirectedToPosttest = true;
        sessionData[sessionId].session.active = false;
        sessionData[sessionId].redirectionToPosttest = {
            sessionId,
            url,
            timestamp: Date.now(),
            params: Object.fromEntries(new URLSearchParams(url.substr(url.indexOf("?")))),
        };
    
        resolve(sessionData[sessionId].redirectionToPosttest);
    });
}

function notifyAnswer(sessionId, questionId, answer) {
    return new Promise((resolve, reject) => {
        if (!sessionExists(sessionId)) {
            return reject("invalid session");
        } else if (!sessionId || !questionId || !answer) {
            return reject("invalid params");
        } else if (!!sessionData[sessionId].answers[questionId]) {
            return reject("already answered");
        }

        sessionData[sessionId].answers[questionId] = {
            sessionId,
            questionId,
            answer,
            timestamp: Date.now(),
        };

        resolve(sessionData[sessionId].answers[questionId]);
    });
}

function notifyTrophyUnlock(sessionId, trophyId) {
    return new Promise((resolve, reject) => {
        if (!sessionExists(sessionId)) {
            return reject("invalid session");
        } else if (!sessionId || !trophyId) {
            return reject("invalid params");
        } else if (!!sessionData[sessionId].trophies[trophyId]) {
            return reject("already earned");
        }

        sessionData[sessionId].trophies[trophyId] = {
            sessionId,
            trophyId,
            timestamp: Date.now(),
        };

        resolve(sessionData[sessionId].trophies[trophyId]);
    });
}

function notifyGame(data = {}) {
    const sessionId = data.sessionId;

    return new Promise((resolve, reject) => {
        if (!sessionExists(sessionId)) {
            return reject("invalid session");
        }

        Object.keys(data).forEach(k => {
            if (["sessionId", "started", "ended", "environmentName", "correctAnswers", "points"].indexOf(k) === -1) {
                delete data[k];
            }
        });

        Object.assign(sessionData[sessionId].game, data);
        
        resolve();
    });
}

function rand(arr) {
    return arr[Math.floor(Math.random() * arr.length)];
}

setInterval(function saveSessions() {
    fs.writeFile("./sessions.json", JSON.stringify(sessionData), e => {
        if (e) throw e;
    });
}, 20 * 1000);

module.exports = {
    getSession,
    createSession,
    closeSession,
    setUserSentToPretest,
    setReturnFromPretest,
    setReturnFromPretestHeaders,
    setUserSentToPosttest,
    notifyAnswer,
    notifyTrophyUnlock,
    notifyGame,
};
