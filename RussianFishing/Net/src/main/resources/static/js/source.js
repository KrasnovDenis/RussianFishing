var botName = "Лизка";

window.onload = function () {
    drawMessage(botName, "Здравствуйте, меня зовут " + botName + " я начинающая рыбачка :) Что вы думаете о рыбалке?");

};

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
    var style = 'style = "width: 500px;"';
    if (user === botName) {
        style = 'style = "width: 500px; margin-left : 50%;"';
    }
    var messageToAppend = '<div ' + style + ' >' + user + ': ' + message + '</div>';
    jQuery(".message_container").append(messageToAppend);

}
