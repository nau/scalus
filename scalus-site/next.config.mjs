import nextra from 'nextra'

/**
 * @type {import('next').NextConfig}
 */
const nextConfig = {
  output: 'export',
  trailingSlash: true,
  images: {
    unoptimized: true // mandatory, otherwise won't export
  },
  // Optional: Change the output directory `out` -> `dist`
  // distDir: "out",
}

const withNextra = nextra({
  latex: true,
  search: {
    codeblocks: false
  },
  contentDirBasePath: '/docs'
})

export default withNextra({
  reactStrictMode: true,
  /*
  async redirects() {
    return [
      {
        source: '/',
        destination: '/docs',
        permanent: true,
      },
    ]
  },*/
})
