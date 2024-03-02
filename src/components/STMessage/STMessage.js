import React from "react";
import { Box, Button } from "@material-ui/core";
import Context from "../../contexts/Context";

export default class STMessage extends React.Component {
    static contextType = Context;

    render() {
        let localization = this.context.environment.localization;
        
        return (
            <Box>
                <Box>
                    <img src={localization.stMessage.img} />
                </Box>

                <center style={{margin: "16px"}}>
                    <Button variant="contained" color="primary" size="large" onClick={this.props.onClickNext}>
                        {localization.stMessage.next}
                    </Button>
                </center>
            </Box>
        );
    }
}