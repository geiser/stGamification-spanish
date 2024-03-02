import React from "react";
import { Box, Button } from "@material-ui/core";

export default class AnswerOptions extends React.Component {
    render() {
        return (
            <Box className="answers">
                {this.props.answers.map((answer, i) => (
                    <Button color="primary" key={answer} onClick={() => this.props.onAnswer(answer)}>
                        {answer}
                    </Button>
                ))}
            </Box>
        );
    }
}
