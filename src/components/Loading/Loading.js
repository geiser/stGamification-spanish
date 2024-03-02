const Loading = ({ fullscreen }) => (
    <>
    {
        !!fullscreen
        ? (
            <div className="dim-screen">
                <div className="loader center"></div>
            </div>
        )
        :
        (
            <div className="loader center"></div>
        )
    }
    </>
);

export default Loading;
