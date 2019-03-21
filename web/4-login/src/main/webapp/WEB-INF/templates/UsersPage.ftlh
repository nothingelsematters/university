<#-- @ftlvariable name="users" type="ru.itmo.webmail.model.domain.User[]" -->
<#import "common.ftl" as c>

<@c.page>
<div class="datatable">
    <div class="caption">Users</div>
    <table>
        <thead>
        <tr>
            <th>#</th>
            <th>id</th>
            <th>Login</th>
            <th>email</th>
        </tr>
        </thead>
        <tbody>
        <#list users as user>
            <tr>
                <td>
                    ${user_index + 1}
                </td>
                <td>
                    ${user.id}
                </td>
                <td>
                    ${user.login}
                </td>
                <td>
                    ${user.email}
                </td>
            </tr>
        </#list>
    </table>
</div>
</@c.page>
