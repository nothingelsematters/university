<template>
    <div class="datatable">
        <div class="caption">Users</div>
        <table>
            <thead>
            <tr>
                <th>ID</th>
                <th>Login</th>
                <th>Name</th>
            </tr>
            </thead>
            <tbody>
            <UserForTable v-for="user in users" :user="user" :key="user.id"/>
            </tbody>
        </table>
    </div>
</template>

<script>
    import UserForTable from './UserForTable'

    export default {
        name: "Users",
        props: ['users'],
        components: {
            UserForTable
        }
    }
</script>

<style scoped>

</style>