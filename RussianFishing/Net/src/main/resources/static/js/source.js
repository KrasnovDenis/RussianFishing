var botName = "Лизка";


window.onload = function () {
    drawMessage(botName, "Здравствуйте, меня зовут " + botName + " я начинающая рыбачка :) Что вы думаете о рыбалке?");
    document.getElementById("content_message").addEventListener("keyup", listener);
};

function listener(event) {
    // Number 13 is the "Enter" key on the keyboard
    if (event.keyCode === 13) {
        // Cancel the default action, if needed
        event.preventDefault();
        // Trigger the button element with a click
        $("#send_message").click();
        $("#content_message").val("");
    }
}

function sendMessage() {
    var content = jQuery('#content_message')[0].value;
    drawMessage('Вы', content);
    var settings = {
        "url": "http://localhost:8080/net_war/chatroom",
        "method": "POST",
        "timeout": 0,
        "headers": {
            "Content-Type": "text/plain"
        },
        "data": content,
    };

    $.ajax(settings).done(function (response) {
        drawMessage(botName, response);
    });
}

function drawMessage(user, message) {
    var style = 'style = "width: 400px;"';
    if (user === botName) {
        style = 'style = "width: 400px; margin-left : 59%;"';
    }
    var messageToAppend = '<div ' + style + ' >' + user + ': ' + message + '</div>';
    jQuery(".message_container").append(messageToAppend);

}
