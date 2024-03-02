import React from "react";
import { AppBar, Typography, Toolbar } from "@material-ui/core";
import "./Header.css";

const Header = ({ title, children }) => (
    <AppBar>
        <Toolbar>
            <Typography variant="h6" className="title">
                {title}
            </Typography>
            {children}
        </Toolbar>
    </AppBar>
);

export default Header;
