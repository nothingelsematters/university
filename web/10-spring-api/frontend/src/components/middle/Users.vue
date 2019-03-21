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
    import axios from 'axios'
    import UserForTable from './UserForTable'

    axios.defaults.baseURL = '/api/1/';

    export default {
        name: "Users",
        props: ['users'],
        components: {
            UserForTable
        },
        beforeMount() {
            axios.get("users").then(response => {
                this.users = response.data;
            });
        }
    }
</script>

<style scoped>

</style>