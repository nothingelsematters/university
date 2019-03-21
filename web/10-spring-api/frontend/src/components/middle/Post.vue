<template>
    <div>
        <PagePostPage :post="post"/>
    </div>
</template>

<script>
    import PagePostPage from "./PostPagePost";
    import axios from "axios";

    export default {
        name: "Post",
        props: ['postId', 'post'],
        components: {
            PagePostPage
        },
        beforeMount() {
            axios.get("notice/" + this.postId).then(response => {
                this.post = response.data;
            });
        }
    }
</script>

<style scoped>

</style>