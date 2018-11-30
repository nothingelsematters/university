window.setMessage = function(message) {
    localStorage.setItem("message", message);
};

window.getMessage = function() {
    return localStorage.getItem("message");
};

window.removeMessage = function() {
    return localStorage.removeItem("message");
};

window.cloneTemplate = function ($template) {
    return $($template.prop('content')).clone();
};
