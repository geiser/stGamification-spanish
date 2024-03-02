const ErrorScreen = (props) => (
    <div className="dim-screen">
        <h1 className="error center">
            {props.message}
        </h1>
    </div>
);

export default ErrorScreen;
