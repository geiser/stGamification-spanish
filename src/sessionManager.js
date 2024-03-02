import axios from "axios";

const storage = localStorage;

var pjson = require('../package.json');
axios.defaults.baseURL = pjson.basename ?? '/';

export { storage };

export function getSessionId() {
    return storage.getItem("sessionId");
}

export function getSessionEnvironment() {
    return storage.getItem("environmentName");
}

export function wasAlreadyRedirectedToPretest() {
    return storage.getItem("redirectedToPretest") === "true";
}

export function validateSession() {
    let sessionId = getSessionId();
    let created = storage.getItem("created");
    let environmentName = getSessionEnvironment();
    let redirectedToPretest = wasAlreadyRedirectedToPretest();

    return axios.get("/validateSession", {
        params: { sessionId, created, environmentName, redirectedToPretest },
    });
}

export function createSession() {
    storage.clear();
    
    return new Promise((resolve, reject) => {
        axios.get("/createSession")
        .then(({ data }) => {
            Object.entries(data).forEach(([key, val]) => storage.setItem(key, val));
            resolve();
        })
        .catch(reject);
    });
}

export function sendUserToPretest(url) {
    return axios.get("/setUserSentToPretest", {
        params: {
            url,
            sessionId: getSessionId(),
        }
    })
    .then(res => {
        storage.setItem("redirectedToPretest", true);

        window.location = url;
    })
    .catch(e => {
        console.error(e);
        // ?????? sessão inválida talvez??
    });
}

export function notifyUserReturnedFromPretest() {
    console.log(`User came back from pre-test; Refeerer: ${document.referrer}`);
    
    return axios.get("/setReturnFromPretest", {
        params: {
            sessionId: getSessionId(),
            referrer: document.referrer,
            url: window.location.href,
        }
    })
    .then(res => {
        storage.setItem("returnedFromPretest", true);
    })
    .catch(e => {
        console.error(e);
        // ?????
    })
}

export function closeSession() {
    return axios.get("/closeSession", {
        params: {
            sessionId: getSessionId(),
        }
    });
}

export function sendUserToPosttest(url) {
    axios.get("/setUserSentToPosttest", {
        params: {
            sessionId: getSessionId(),
            url,
        }
    })
    .catch(e => {
        console.error(e);
    })
    .finally(() => {
        storage.setItem("redirectedToPosttest", true);
        storage.clear();

        window.location = url;
    });
}

export function notifyAnswer(questionId, answer) {
    return axios.get("/notifyAnswer", {
        params: {
            sessionId: getSessionId(),
            questionId,
            answer,
        }
    });
}

export function notifyTrophyUnlock(trophyId) {
    return axios.get("/notifyTrophyUnlock", {
        params: {
            sessionId: getSessionId(),
            trophyId,
        }
    });
}

export function notifyGame(params) {
    params.sessionId = getSessionId();

    return axios.post("/notifyGame", params);
}
