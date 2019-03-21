<#import "./common.ftlh" as c>

<@c.page>
<div class="datatable">
    <div class="caption">Users</div>
    <table>
        <thead>
        <tr>
            <th>ID</th>
            <th>Login</th>
            <th>Created At</th>
        </tr>
        </thead>
        <tbody>
    <#list users as user>
    <tr>
        <td>${user.id}</td>
        <td>${user.login}</td>
        <td>${user.creationTime}</td>
    </tr>
    </#list>
        </tbody>
    </table>
</div>
</@c.page>
