
function sendMessage() {
    var content = jQuery('#content_message')[0].value;
    drawMessage('Вы', content);
    $.ajax({
        url: "http://localhost:8080/net_war/chatroom",
        type: 'post',
        dataType: 'json',
        contentType: 'application/json',
        data: JSON.stringify( { 'message' : content}),

    }).done(function (data) {
        drawMessage('Денис', data);
    });
}

function drawMessage(user, message) {
    var messageToAppend = '<div>' + user + ': ' + message + '</div>';
    jQuery(".message_container").append(messageToAppend);
}

