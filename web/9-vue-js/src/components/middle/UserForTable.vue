<template>
    <tr>
        <td>{{ user.id }}</td>
        <td>{{ user.login }}</td>
        <td>{{ user.name }}</td>
    </tr>
</template>

<script>
    export default {
        name: "UserForTable",
        props: ['user']
    }
</script>

<style scoped>

</style>