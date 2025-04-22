/* eslint-env node */
import { Footer, Layout, Navbar } from 'nextra-theme-docs'
import { Banner, Head } from 'nextra/components'
import { getPageMap } from 'nextra/page-map'
import 'nextra-theme-docs/style.css'

export const metadata = {
  metadataBase: new URL('https://scalus.org'),
  title: {
    template: '%s - Scalus'
  },
  description: 'Scalus: DApps Development Platform for Cardano',
  applicationName: 'Scalus',
  generator: 'Next.js',
  appleWebApp: {
    title: 'Scalus'
  },
  other: {
    'msapplication-TileImage': '/ms-icon-144x144.png',
    'msapplication-TileColor': '#fff'
  },
  twitter: {
    site: 'https://scalus.org'
  }
}

export default async function RootLayout({ children }) {
  const navbar = (
    <Navbar
      logo={
        <div style={{ display: 'flex', alignItems: 'center' }}>
          <img src="/scalus-logo-dark.png" alt="Logo" style={{ height: '2.5rem', marginRight: '0.5rem' }} />
          {/* <b>Scalus</b>{'   '}
          <span style={{ opacity: '60%' }}>DApps Development Platform for Cardano</span> */}
        </div>
      }
      projectLink="https://github.com/nau/scalus"
      // Lantr discord server / Scalus channel
      chatLink="https://discord.gg/32Qgvz4s"
    />
  )
  const pageMap = await getPageMap()
  return (
    <html lang="en" dir="ltr" suppressHydrationWarning>
      <Head faviconGlyph="✦" />
      <body>
        <Layout
          // banner={<Banner storageKey="Nextra 2">Scalus News Banner</Banner>}
          navbar={navbar}
          footer={<Footer>Apache 2.0 © {new Date().getFullYear()} Lantr Labs</Footer>}
          // editLink="Edit this page on GitHub"
          docsRepositoryBase="https://github.com/nau/scalus"
          sidebar={{ defaultMenuCollapseLevel: 1 }}
          pageMap={pageMap}
        >
          {children}
        </Layout>
      </body>
    </html>
  )
}
