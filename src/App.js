import React from "react";
import { MuiThemeProvider } from "@material-ui/core/styles";
import { defineTheme } from "./utils/themes";
import { ErrorScreen, Game, Loading } from "./components";
import { createSession, getSessionEnvironment, wasAlreadyRedirectedToPretest, getSessionId, sendUserToPretest, notifyUserReturnedFromPretest } from "./sessionManager";
import { default as config } from  "./utils/configuration";
import Context from "./contexts/Context";
import './App.css';

export default class App extends React.Component {
    constructor(props) {
        super(props);

        this.state = {
            ready: false,
            environment: null,
            errorMessage: null,

            getPlayer: null,
            setPlayer: (player, cb) => this.setState({ getPlayer: player }, cb),
        };

        this.theme = null;

        this.setReady = this.setReady.bind(this);
        this.mountErrorScreen = this.mountErrorScreen.bind(this);
    }

    componentDidMount() {
        const envName = getSessionEnvironment();
        const wasRedirectedToPretest = wasAlreadyRedirectedToPretest();

        // sanity check; no envname = no session
        if (!envName) {
            // creates new session and reloads the page
            return createSession().finally(() => {
                window.location.reload();
            });
        }

        config.getEnvironment(envName)
        .then(environment => {
            document.title = environment.localization.pageTitle;
            
            if (environment.preTest) {
                // redirects user to pretest
                if (!wasRedirectedToPretest) {
                    const pretestUrl = environment.preTest
                        .replace(/\{\{sessionId\}\}/g, getSessionId())
                        .replace(/\{\{points\}\}/g, 0) // ???????
                    ;

                    return sendUserToPretest(pretestUrl);
                }
                // user has returned from pretest
                else {
                    notifyUserReturnedFromPretest();
                }
            } else {
                console.warn(`Pretest for "${environment.name}" is not defined in "${environment.file}"`);
            }

            this.theme = defineTheme(environment.theme);
            this.setState({ environment }, this.setReady);
        })
        .catch(e => {
            this.mountErrorScreen(e.toString());
            console.error(e);
        });
    }

    setReady() {
        this.setState({ ready: true });
    }

    mountErrorScreen(message) {
        if (typeof message === "object")
            message = message.message || message.errorMessage;
        
        this.setState({ errorMessage: message });
    }

    render() {
        if (this.state.errorMessage) {
            return ( <ErrorScreen message={this.state.errorMessage} /> );
        }

        // waiting for environment
        if (!this.state.ready) {
            return ( <Loading fullscreen={true} /> );
        }

        return (
            <Context.Provider value={this.state}>
                <MuiThemeProvider theme={this.theme}>
                    <Game />
                </MuiThemeProvider>
            </Context.Provider>
        );
    }
}
