import React from "react";
import { Box, Typography } from "@material-ui/core";
import "./PlayerStats.css";
import Context from "../../contexts/Context";

export default class PlayerStats extends React.Component {
    static contextType = Context;

    render() {
        let { localization } = this.context.environment;
        let player = this.context.getPlayer();

        return (
            <Box className="game-box game-box-stats">
                <Box className="game-box game-box-stats-item">
                    <Typography variant="h5">
                        {localization.points}
                    </Typography>
                    <Typography variant="h4" style={{textAlign: "center"}}>
                        {player.points}
                    </Typography>
                </Box>

                <Box className="game-box game-box-stats-item">
                    <Typography variant="h5">
                        {localization.rightAnswers}
                    </Typography>
                    <Typography variant="h4" style={{textAlign: "center"}}>
                        {player.correctAnswers}
                    </Typography>
                </Box>
            </Box>
        );
    }
}
