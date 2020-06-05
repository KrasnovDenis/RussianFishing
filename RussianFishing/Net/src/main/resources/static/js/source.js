
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
        drawMessage('Лизка', response);
    });
}

function drawMessage(user, message) {
    var messageToAppend = '<div>' + user + ': ' + message + '</div>';
    jQuery(".message_container").append(messageToAppend);
}

