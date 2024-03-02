import { Box } from "@material-ui/core";
import React from "react";

export default class Trophy extends React.Component {
    render() {
        return (
            <Box className="trophies-list-item">
                <img
                    src={this.props.unlocked ? this.props.image : this.props.lockedImage}
                    alt=""
                />
            </Box>
        );
    }
}
