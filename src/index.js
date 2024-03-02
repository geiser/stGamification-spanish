import React from "react";
import ReactDOM from "react-dom";
import App from "./App";
import reportWebVitals from "./reportWebVitals";
import { createSession, validateSession } from "./sessionManager";

(function() {
    const storage = window.localStorage;

    Promise.resolve()
        .then(validateSession)
            .catch(createSession)
        .then(() => console.table(storage))
        .finally(renderWebpage)
    ;

    function renderWebpage() {
        ReactDOM.render(
            <React.StrictMode>
                <App />
            </React.StrictMode>,
            document.getElementById('root')
        );
    }
})();

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
