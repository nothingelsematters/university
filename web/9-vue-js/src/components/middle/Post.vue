<template>
    <div>
        <PagePostPage :post="getPost()" :users="users"/>
    </div>
</template>

<script>
    import PagePostPage from "./post/PostPagePost"

    export default {
        name: "Post",
        props: ['posts', 'users', 'postId'],
        components: {
            PagePostPage
        },
        methods: {
            getPost: function() {
                return this.posts[this.postId];
            }
        }
    }
</script>

<style scoped>

</style>