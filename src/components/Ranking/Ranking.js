import React from "react";
import { Box, Typography } from "@material-ui/core";
import { RankingPlayer } from "../";
import "./Ranking.css";
import Context from "../../contexts/Context";

export default class Ranking extends React.Component {
    static contextType = Context;

    render() {
        let { ranking, localization } = this.context.environment;

        let player = this.context.getPlayer();
        
        let players = [...ranking, {
            name: player.username,
            avatar: player.avatar,
            points: player.points,
        }]
        .sort((a, b) => b.points - a.points)
        .map((player, i) => (
            <RankingPlayer
                key={player.name}
                name={player.name}
                avatar={player.avatar}
                points={player.points}
                position={i + 1}
            />
        ));

        return (
            <Box className="game-container-ranking">
                <Typography variant="h5">{localization.ranking}</Typography>
                
                <Box className="ranking-box">
                    {players}
                </Box>
            </Box>
        );
    }
}
