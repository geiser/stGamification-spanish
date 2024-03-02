import React from "react";
import { Box, Typography } from "@material-ui/core";
import { STMessage, AvatarSelection, Loading, Question } from "../";
import { closeSession, getSessionId } from "../../sessionManager";
import { sendUserToPosttest, notifyAnswer, notifyGame } from "../../sessionManager";
import Context from "../../contexts/Context";
import "./Quiz.css";

export default class Quiz extends React.Component {
    static contextType = Context;

    constructor(props) {
        super(props);

        this.state = {
            ready: false,
            headerText: null,
            textContent: null,
            phase: "",
            avatarList: [],
        };

        this.changeHeader = this.changeHeader.bind(this);
        this.changeText = this.changeText.bind(this);
        this.startSTMessage = this.startSTMessage.bind(this);
        this.startAvatarSelection = this.startAvatarSelection.bind(this);
        this.startQuiz = this.startQuiz.bind(this);
        this.onAnswer = this.onAnswer.bind(this);
        this.onFinish = this.onFinish.bind(this);
    }

    componentDidMount() {
        let env = this.context.environment;

        this.setState({
            ready: true,
            avatarList: env.avatarList,
        });

        if (env.localization.stMessage != undefined) {
            this.startSTMessage();
        } else {
            this.startAvatarSelection();
        }

        notifyGame({ environmentName: env.name });
    }

    changeHeader(headerText) {
        this.setState({ headerText });
    }

    changeText(textContent) {
        this.setState({ textContent });
    }

    startSTMessage() {
        let env = this.context.environment;

        this.setState({
            phase: "onSTMessage",
            headerText: env.localization.stMessage.header,
            textContent: env.localization.stMessage.text,
        });
    }

    startAvatarSelection() {
        let env = this.context.environment;

        this.setState({
            phase: "onAvatarSelection",
            headerText: env.localization.avatarSelection.header,
            textContent: env.localization.avatarSelection.text,
            avatarList: env.avatarList,
        });
    }

    startQuiz() {
        let localization = this.context.environment.localization;

        this.setState({
            phase: "onQuiz",
            headerText: localization.quiz.header,
            textContent: localization.quiz.text,
        });

        notifyGame({ started: Date.now() });
    }

    onAnswer({ questionId, answer }) {
        notifyAnswer(questionId, answer);
    }

    onFinish() {
        let env = this.context.environment;

        notifyGame({ ended: Date.now() });

        if (env.postTest) {
            const posttestUrl = env.postTest
                .replace(/\{\{sessionId\}\}/g, getSessionId())
                .replace(/\{\{points\}\}/g, this.context.getPlayer().points)
            ;

            sendUserToPosttest(posttestUrl);
        } else {
            console.warn(`Posttest for "${env.name}" is not defined in "${env.file}"`);

            closeSession();
        }
    }

    render() {
        let player = this.context.getPlayer();
        let content;

        if (!this.state.ready) {
            content = ( <Loading /> );
        } else if (this.state.phase == "onSTMessage") {
            content = ( <STMessage onClickNext={this.startAvatarSelection} /> );
        } else if (this.state.phase == "onAvatarSelection") {
            content = (
                <AvatarSelection
                    setAvatar={player.setAvatar}
                    avatars={this.state.avatarList}
                    onClickNext={this.startQuiz}
                />
            );
        } else if (this.state.phase == "onQuiz") {
            content = (
                <Question
                    onAnswer={this.onAnswer}
                    onFinish={this.onFinish}
                />
            );
        }

        return (
            <Box className="quiz-box">
                <Typography variant="h5" className="text-header">
                    {this.state.headerText}
                </Typography>
                <Typography variant="h6" className="text-content">
                    {this.state.textContent}
                </Typography>

                {content}
            </Box>
        );
    }
}
