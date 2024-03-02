import React from "react";
import { Box, Typography } from "@material-ui/core";
import { Trophy } from "../";
import Context from "../../contexts/Context";
import "./Trophies.css";

export default class Trophies extends React.Component {
    static contextType = Context;

    render() {
        return (
            <Box className="trophies-container">
                <Typography variant="h5">
                    {this.context.environment.localization.myTrophies}
                </Typography>

                <Box className="trophies-list">
                    {
                        this.context.environment.trophies.map(trophy => (
                            <Trophy
                                key={trophy.id}
                                {...trophy}
                            />
                        ))
                    }
                </Box>
            </Box>
        );
    }
}
