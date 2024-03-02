import { createTheme } from "@material-ui/core/styles";
import * as colors from "@material-ui/core/colors";

// https://material-ui.com/customization/theming/#createtheme-options-args-theme
// https://material-ui.com/customization/palette/#providing-the-colors-directly

export function defineTheme(theme) {
    let { primary, secondary } = theme;

    if (!secondary)
        secondary = primary;

    if (typeof primary === "string") {
        primary = colors[primary];
    } else if (typeof primary === "object" && primary.extends) {
        let _extends = primary.extends;
        delete primary.extends;

        Object.assign(colors[_extends], primary);
    }

    if (typeof secondary === "string") {
        secondary = colors[secondary];
    } else if (typeof secondary === "object" && secondary.extends) {
        let _extends = secondary.extends;
        delete secondary.extends;

        Object.assign(colors[_extends], secondary);
    }

    return createTheme({
        palette: {
            primary,
            secondary,
        }
    });
}
