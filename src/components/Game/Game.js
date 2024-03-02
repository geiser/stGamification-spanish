import React from "react";
import { Container, Box } from "@material-ui/core";
import { Player, Header, PlayerStats, Ranking, Quiz, Trophies, Loading } from "../";
import Context from "../../contexts/Context";
import "./Game.css";
import { notifyTrophyUnlock, notifyGame } from "../../sessionManager";

export default class Game extends React.Component {
    static contextType = Context;

    constructor(props) {
        super(props);
        
        this.getPlayer = this.getPlayer.bind(this);
        this.setAvatar = this.setAvatar.bind(this);
        this.updatePoints = this.updatePoints.bind(this);
        this.updateTrophies = this.updateTrophies.bind(this);
        this.updateCollectedData = this.updateCollectedData.bind(this);
        this.updateQuestions = this.updateQuestions.bind(this);

        this.state = {
            ready: false,
            username: "Yo",
            avatar: "./assets/default/images/avatar.svg",
            testType: null,
            points: 0,
            correctAnswers: 0,
            level: 0,
            questions: [],
            trophies: [],
            collectedData: [],

            setAvatar: this.setAvatar,
            updatePoints: this.updatePoints,
            updateTrophies: this.updateTrophies,
            updateCollectedData: this.updateCollectedData,
            updateQuestions: this.updateQuestions,
        };

        /*
        // adds a getter called "player"
        Object.defineProperty(this, "player", {
            get: function() { return this.state }.bind(this),
        });
        */
    }

    getPlayer() {
        return this.state;
    }

    componentDidMount() {
        let env = this.context.environment;

        this.setState({
            testType: env.name,
            avatar: env.avatarList[0],
            trophies: env.trophies.map(trophy => Object.assign(trophy, { unlocked: false }))
        }, () => {
            this.context.setPlayer(this.getPlayer, () => {
                this.setState({ ready: true });
            });
        });
    }

    setAvatar(avatar) {
        this.setState({ avatar });
    }

    updatePoints(value) {
        let up = value > 0 ? 1 : 0;

        this.setState(prevState => ({
            points: Math.max(0, prevState.points + value),
            level: prevState.level + 1,
            correctAnswers: prevState.correctAnswers + up,
        }), () => {
            this.updateTrophies();
        });

        notifyGame({ correctAnswers: this.state.correctAnswers });
        notifyGame({ points: this.state.points });
    }

    updateTrophies() {
        let trophyList = {};
        this.state.trophies.forEach(trophy => trophyList[trophy.id] = trophy);

        for (const [id, trophy] of Object.entries(trophyList)) {
            if (this.state.points >= trophy.unlockAt.points
                && this.state.correctAnswers >= trophy.unlockAt.correctAnswers
                && this.state.level >= trophy.unlockAt.question
                && !trophyList[id].unlocked
            ) {
                trophyList[id].unlocked = true;
                trophyList[id].timestamp = +Date.now();

                this.setState({
                    trophies: Object.values(trophyList),
                });

                notifyTrophyUnlock(id);
            }
        }
    }

    updateCollectedData() {
        // TO DO
    }

    updateQuestions({ questionId, answer }) {
        // TO DO
    }

    render() {
        let env = this.context.environment;

        if (!this.state.ready) {
            return ( <Loading fullscreen={true} /> );
        }

        return (
            <Box className="game">
                {/* Header */}
                <Header title={env.localization.title}>
                    <Player
                        username={this.state.username}
                        avatar={this.state.avatar}
                    />
                </Header>

                {/* Game */}
                <Container maxWidth={false} className="game-container">
                    <Box className="game-container-box">
                        <PlayerStats />

                        <Ranking />
                    </Box>

                    <Quiz />
                    
                    <Trophies />
                </Container>
            </Box>
        );
    }
}
