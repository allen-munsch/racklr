// Simulate what the page's getStaticProps does, calling lib/api directly
import { getAllPosts } from './lib/api.ts';
import { CMS_NAME } from './lib/constants.ts';

const getStaticProps = async () => {
  const allPosts = getAllPosts([
    'title', 'date', 'slug', 'author', 'coverImage', 'excerpt',
  ]);
  return { props: { allPosts } };
};

getStaticProps().then(r => console.log('__RACKLR_PROPS__' + JSON.stringify(r)));
