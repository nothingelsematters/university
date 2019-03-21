<#-- @ftlvariable name="users" type="ru.itmo.webmail.model.domain.User[]" -->
<#import "common.ftl" as c>

<@c.page>
<div class="datatable">
    <div class="caption">Users</div>
    <table>
        <thead>
        <tr>
            <th>#</th>
            <th>Login</th>
        </tr>
        </thead>
        <tbody>
        <#list users as user>
            <tr>
                <td>
                    ${user_index + 1}
                </td>
                <td>
                    ${user.login}
                </td>
            </tr>
        </#list>
    </table>
</div>
</@c.page>
