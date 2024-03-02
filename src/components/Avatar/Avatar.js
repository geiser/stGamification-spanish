import React from "react";
import "./Avatar.css";

export default class Avatar extends React.Component {
    constructor(props) {
        super(props);

        this.setAvatar = function() {
            if (!!props.setAvatar) {
                props.setAvatar(props.avatar);
            }
        }
    }
    render() {
        return (
            <div className="avatar" onClick={this.setAvatar}>
                <img
                    src={this.props.avatar}
                    alt=""
                />
            </div>
        )
    }
}
