<#import "common.ftl" as c>

<@c.page>
<div class="register form-box">
    <div class="header">Register</div>
    <div class="body">
        <form method="post" action="/register">
            <input type="hidden" name="action" value="register">
            <div class="field">
                <div class="name">
                    <label for="login">Login</label>
                </div>
                <div class="value">
                    <input id="login" name="login" value="${login!}"/>
                </div>
            </div>
            <div class="field">
                <div class="name">
                    <label for="password">Password</label>
                </div>
                <div class="value">
                    <input id="password" type="password" name="password" value="${password!}"/>
                </div>
            </div>
            <div class="button-field">
                <input type="submit" value="Register">
            </div>
            <div class="error"></div>
        </form>
    </div>
</div>
<script>
    $(function() {
        $(".register form").submit(function () {
            var login = $(this).find("input[name='login']").val();
            var password = $(this).find("input[name='password']").val();
            var $error = $(this).find(".error").text("");
            $.post("", {action: "register", login: login, password: password}, function (response) {
                if (response["success"]) {
                    setMessage("You are registered. Welcome!");
                    document.location.href = "/";
                } else {
                    $error.text(response["error"]);
                }
            });
            return false;
        });
    })
</script>
</@c.page>
