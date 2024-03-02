import React from "react";
import { Box, Typography } from "@material-ui/core";
import { AnswerOptions } from "../";
import Context from "../../contexts/Context";
import "./Question.css";

export default class Question extends React.Component {
    static contextType = Context;

    constructor(props) {
        super(props);

        this.state = {
            ready: false,
            currentQuestion: null,
            questions: [],
            questionIndex: 0,
            showMessage: null,
            text: null,
        };

        this.feedbackTimeout = null;
        this.valid = true;
        this.answers = {};

        this.checkAnswer = this.checkAnswer.bind(this);
        this.nextQuestion = this.nextQuestion.bind(this);
    }

    componentDidMount() {
        let env = this.context.environment;

        this.setState({
            ready: true,
            currentQuestion: env.questions[0],
            questions: env.questions,
        });
    }

    // alias
    get currentQuestion() {
        return this.state.currentQuestion;
    }

    // alias
    get questions() {
        return this.state.questions;
    }

    // alias
    get player() {
        return this.context.getPlayer();
    }

    checkAnswer(answer) {
        if (!this.valid)
            return;

        const playSound = this.context.environment.playSound;

        this.props.onAnswer({
            questionId: this.currentQuestion.id,
            answer: answer,
        });
        
        clearTimeout(this.feedbackTimeout);
        
        // right
        if (answer === this.currentQuestion.rightAnswer) {
            this.player.updatePoints(10);
            this.setState({ showMessage: "right" });

            if (playSound) {
                new Audio('./assets/default/audio/right.mp3').play();
            }
        }
        // wrong
        else {
            this.player.updatePoints(-5);
            this.setState({ showMessage: "wrong" });

            if (playSound) {
                new Audio('./assets/default/audio/wrong.mp3').play();
            }
        }

        function hide() { this.setState({ showMessage: null }) }
        this.feedbackTimeout = setTimeout(hide.bind(this), 2000);

        this.answers[this.currentQuestion.id] = answer;

        this.nextQuestion();
    }

    nextQuestion() {
        if (this.state.questionIndex >= this.questions.length - 1) {
            this.valid = false;
            this.props.onFinish();
            // TO DO
            return;
        }

        this.setState(prevState => ({
            currentQuestion: this.questions[prevState.questionIndex + 1],
            questionIndex: prevState.questionIndex + 1,
            text: this.currentQuestion.text,
        }));
    }

    render() {
        if (!this.state.ready) {
            return ( <Box className="loader center" style={{borderTopColor: "#fff"}}></Box> );
        }

        let feedbackStyle = {
            display: !!this.state.showMessage ? "block" : "none",
            background: this.state.showMessage === "right" ? "#238823" : "#d2222d",
        };

        let localization = this.context.environment.localization;

        return (
            <>
                <Box className="question-box">
                    <Box className="question">
                        <Typography variant="h6">
                            Pregunta {this.state.questionIndex + 1}/{this.questions.length}
                        </Typography>

                        {this.currentQuestion.text}
                                
                        <img src={this.currentQuestion.image} alt="" />
                    </Box>

                    <AnswerOptions
                        answers={this.currentQuestion.answers}
                        onAnswer={this.checkAnswer}
                    />
                </Box>

                <div className="question-box feedback" style={feedbackStyle}>
                    {this.state.showMessage === "right" ? localization.rightAnswer : localization.wrongAnswer}
                </div>
            </>
        );
    }
}
