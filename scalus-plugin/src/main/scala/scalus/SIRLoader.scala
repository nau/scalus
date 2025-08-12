package scalus

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.io.ClassPath
import scalus.flat.DecoderState
import scalus.flat.FlatInstantces.given
import scalus.sir.Module

import java.io.InputStream
import java.net.URL
import scala.util.control.NonFatal

case class SIRLoaderOptions(
    classPath: List[String],
    /** additional directory */
    additionalUrl: Option[URL],
    /** predefined sir-s ifexists */
    predefinedSirs: Option[String],
)

class SIRLoader(options: SIRLoaderOptions):

    private val classLoader: ClassLoader = makeClassLoader

    def findAndReadModule(
        moduleName: String,
        debug: Boolean = true
    ): Either[String, Module] = {
        val filename = moduleName.replace('.', '/') + ".sir"
        findAndReadModuleFromClassloader(filename) match
            case Left(filename) =>
                options.predefinedSirs match {
                    case None =>
                        if debug then println(s"read for ${filename} failed")
                        Left(filename)
                    case Some(predefinedSirs) =>
                        if debug then
                            println(s"read for ${filename} failed, look at predefined-sirs")
                        val file = new java.io.File(s"${predefinedSirs}/${filename}")
                        if file.exists() then
                            findAndReadModuleFromFile(
                              new java.io.File(s"${predefinedSirs}/${filename}"),
                              debug
                            )
                        else Left(filename)
                }
            case r @ Right(module) => r
    }

    def findAndReadModuleFromClassloader(
        filename: String,
        debug: Boolean = false
    ): Either[String, Module] = {
        if debug then println(s"findAndReadModule: ${filename}")
        // read the file from the classpath
        val resource = classLoader.getResourceAsStream(filename)
        if resource != null then
            try
                Right(parseInputStream(resource))
            finally
                resource.close()
        else Left(filename)
    }

    def findAndReadModuleFromFile(
        file: java.io.File,
        debug: Boolean = false
    ): Either[String, Module] = {
        if debug then println(s"findAndReadModule: ${file.toPath}")
        // read the file from the classpath
        val fileInputStream = new java.io.FileInputStream(file)
        try Right(parseInputStream(fileInputStream))
        catch
            case NonFatal(ex) =>
                Left(s"${file.toPath} [${ex.getMessage}]")
        finally fileInputStream.close()
    }

    def parseInputStream(input: InputStream): Module = {
        val buffer = input.readAllBytes()
        val dec = DecoderState(buffer)
        val module = flat.decode[Module](dec)
        module
    }

    private def makeClassLoader: ClassLoader = {
        import scala.language.unsafeNulls
        val entries = options.classPath
        // val entries = ClassPath.expandPath(ctx.settings.classpath.value, expandStar = true)
        val urls = entries.map(cp => java.nio.file.Paths.get(cp).toUri.toURL)
        val out = options.additionalUrl.toList
        val allUrls = urls ++ out
        new java.net.URLClassLoader(allUrls.toArray, getClass.getClassLoader)
    }

end SIRLoader
