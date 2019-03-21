<#-- @ftlvariable name="users" type="ru.itmo.webmail.model.domain.User[]" -->
<#import "common.ftl" as c>

<@c.page>
<div class="datatable users">
    <div class="caption">Users</div>
    <table>
        <thead>
        <tr>
            <th>#</th>
            <th>Login</th>
            <th>Rating</th>
            <#if admin??>
                <th>Manage Admin Rights</th>
            </#if>
        </tr>
        </thead>
        <tbody>
        <tr>
            <td colspan="3" class="no-data">No data</td>
        </tr>
        </tbody>
    </table>
</div>

<template class="users-tr">
    <tr>
        <td class="id">{{#}}</td>
        <td class="login">{{login}}</td>
        <td class="rating"></td>
        <#if admin??>
            <td class="manage">
                <form method="post" action="/users">
                    <input type="hidden" name="action" value="change">
                    <input type="submit" class="main">
                </form>
                <div class="error"></div>
            </td>
        </#if>
    </tr>
</template>

<script>
    $(function () {
        var $noData = $(".users .no-data");
        $noData.text("Loading...");
        $.post("", {action: "find"}, function (users) {
            var $tbody = $(".users tbody");
            if (users.length > 0) {
                var logins = [];
                $tbody.empty();
                users.forEach(function (user) {
                    var $tr = cloneTemplate($("template.users-tr"));
                    $tr.find(".id").text(user.id);
                    $tr.find(".login").text(user.login);
                    $tr.find(".main").attr("value", (user.admin ? "disable" : "enable"));
                    $tr.find(".main").click(function() {
                        var userId = user.id;
                        var $button = $(this);
                        var changing = $button.attr("value");
                        var $error = $(this).closest("tr").find(".error");

                        $.post("", {action: "change", userId: userId, changing: changing}, function (response) {
                            if (response["success"]) {
                                $button.attr("value", (changing == "enable" ? "disable" : "enable"));
                            } else {
                                $error.text(response["error"]);
                            }
                        });
                        return false;
                    });
                    $tr.find("tr").attr("id", "user_" + user.login.toLowerCase());
                    logins.push(user.login);
                    $tbody.append($tr);
                });
                $.ajax({
                    url: "http://codeforces.com/api/user.info",
                    jsonp: "jsonp",
                    dataType: "jsonp",
                    data: {
                        handles: logins.join(";")
                    },
                    success: function(response) {
                        if (response["status"] === "OK") {
                            response["result"].forEach(function (user) {
                                $("#user_" + user.handle.toLowerCase()).find(".rating").text(user["rating"]);
                            })
                        }
                    }
                })
            } else {
                $noData.text("No data");
            }
        })
    })
</script>

</@c.page>
