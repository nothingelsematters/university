<template>
    <section>
        <div class="header">
            By {{users[post.userId].login}}
        </div>
        <div class="body">
            {{post.text}}
        </div>
        <div class="footer">
            <a :href="'#page=Post' + post.id" @click="postRedirect(post.id)">View all</a>
        </div>
    </section>
</template>

<script>
    export default {
        props: ['users', 'post'],
        name: "SidebarPost",
        methods: {
            postRedirect: function(postId) {
                this.$root.$emit("onChangePage", 'Post' + postId);
            }
        },
    }
</script>

<style scoped>

</style>
