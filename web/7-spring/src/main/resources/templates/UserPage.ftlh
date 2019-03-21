<#import "./common.ftlh" as c>

<@c.page>
<#if userToShow??>
    <div class="datatable">
        <div class="caption">${userToShow.login}'s properties</div>
        <table>
            <thead>
            <tr>
                <th>ID</th>
                <th>Login</th>
                <th>Created At</th>
            </tr>
            </thead>
            <tbody>
            <tr>
                <td>${userToShow.id}</td>
                <td>${userToShow.login}</td>
                <td>${userToShow.creationTime}</td>
            </tr>
            </tbody>
        </table>
    </div>
<#else>
    No Such User
</#if>
</@c.page>
