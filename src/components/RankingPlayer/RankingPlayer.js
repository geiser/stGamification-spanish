import React from "react";
import { Box, Typography } from "@material-ui/core";
import { Avatar } from "../";
import "./RankingPlayer.css";


export default class RankingPlayer extends React.Component {
    render() {
        return (
            <Box className="ranking-player">
                <Box className="player">
                    <Typography variant="h6">
                        {this.props.position}.
                    </Typography>

                    <Avatar avatar={this.props.avatar} />

                    <Typography variant="h6">
                        {this.props.name}
                    </Typography>
                </Box>

                <Box className="points">
                    <Box className="points-bar-container">
                        <Box className="points-bar" style={{width: (this.props.points / 2) + "%"}} bgcolor="primary.main"></Box>
                        <Typography variant="h6">
                            {this.props.points}
                        </Typography>
                    </Box>
                </Box>
            </Box>
        )
    }
}
