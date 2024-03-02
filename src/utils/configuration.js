import { default as config } from "../config.json";

export default class Config {
    static getVar(key) {
        return config[key];
    }

    static getEnvironments() {
        return Object.keys(config.environments);
    }

    static async readConfigFile(file) {
        return await require("../environments/" + file);
    }

    static async getRawEnvironment(envName) {
        let env = config.environments[envName];

        // undefined
        if (!env) {
            throw new Error(`environment "${envName}" is not defined`);
        }
        // read from file
        else if (typeof env === "string") {
            return await this.readConfigFile(env);
        }
        // literal object
        else if (typeof env === "object") {
            return env;
        }
        // anything else
        else {
            throw new Error(`invalid environment: "${envName}"`);
        }
    }

    static async getDefaultEnvironment() {
        return this.getRawEnvironment("default");
    }

    static async getEnvironment(envName) {
        let env = await this.getRawEnvironment(envName);

        let defaultEnv = await this.getDefaultEnvironment();

        // merge with default
        env = Object.assign({}, defaultEnv, env, {
            name: envName,
            file: config.environments[envName],
        });

        // merge localization
        if (!'localization' in env)
            Object.assign(env.localization, defaultEnv.localization);

        env = this.replaceLinks(env);
        
        return env;
    }

    static replaceLinks(env) {
        function replace(str) {
            return str
                .replace(/\{theme\}/g, env.name)
            ;
        }

        // ranking
        env.ranking = env.ranking.map(player => Object.assign(player, { avatar: replace(player.avatar) }));

        // avatar list
        env.avatarList = env.avatarList.map(avatar => replace(avatar));

        // questions
        env.questions = env.questions.map(question => Object.assign(question, { image: replace(question.image) }));

        // trophies
        env.trophies = env.trophies.map(trophy => Object.assign(trophy, {
            lockedImage: replace(trophy.lockedImage),
            image: replace(trophy.image)
        }));

        return env;
    }
}
