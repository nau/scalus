import nextra from 'nextra'
 
/**
 * @type {import('next').NextConfig}
 */
const nextConfig = {
  output: 'export',
  //reactStrictMode: true,
  // trailingSlash: true,

  images: {
    unoptimized: true // mandatory, otherwise won't export
  }, 
  // Optional: Change the output directory `out` -> `dist`
  // distDir: "build"
}
const withNextra = nextra({
  // ... other Nextra config options
  search: {
    codeblocks: false
  },
  contentDirBasePath: '/docs'
})
 
export default withNextra(nextConfig)