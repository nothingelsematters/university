<template>
    <div class="middle">
        <aside>
            <SidebarPost v-for="post in posts.slice().reverse()" :post="post" :key="post.id"/>
        </aside>
        <main>
            <Index v-if="page === 'Index'"/>
            <Enter v-if="page === 'Enter'"/>
            <Register v-if="page === 'Register'"/>
            <AddPost v-if="page === 'AddPost'"/>
            <Users v-if="page === 'Users'"/>
            <Post v-if=/Post*/.test(page) :postId="page.slice(4)"/>
        </main>
    </div>
</template>

<script>
    import Index from './middle/Index';
    import Enter from './middle/Enter';
    import Register from './middle/Register';
    import AddPost from './middle/AddPost';
    import SidebarPost from './SidebarPost';
    import Users from './middle/Users';
    import Post from './middle/Post';
    import axios from 'axios';

    export default {
        name: "Middle",
        props: ['posts'],
        data: function () {
            return {
                page: "Index"
            }
        },
        beforeCreate() {
            this.$root.$on("onChangePage", (page) => {
                this.page = page;
            });

            axios.get("notices").then(response => {
                this.posts = response.data;
            });
        },
        components: {
            Index,
            Enter,
            Register,
            SidebarPost,
            AddPost,
            Users,
            Post
        }
    }
</script>

<style scoped>

</style>
