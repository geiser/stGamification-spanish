import React from "react";
import { Typography } from "@material-ui/core";
import { Avatar } from "../";

export default class Player extends React.Component {
    render() {
        return (
            <Typography variant="h6" className="player">
                {this.props.username}
                <Avatar
                    avatar={this.props.avatar}
                />
            </Typography>
        );
    }
}
