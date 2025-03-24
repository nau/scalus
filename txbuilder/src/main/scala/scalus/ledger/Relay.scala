package scalus.ledger

import io.bullet.borer.{Decoder, Dom, Encoder, Reader, Writer}
import scalus.builtin.ByteString

/** Represents a relay for a stake pool in the Cardano blockchain.
  *
  * A relay is a network endpoint for a stake pool that can be one of three types:
  *   - SingleHostAddr: An IP address with optional port
  *   - SingleHostName: A DNS name with optional port
  *   - MultiHostName: A DNS name for SRV lookup
  */
enum Relay {

    /** A relay specified by an IP address.
      *
      * @param port
      *   Optional port number
      * @param ipv4
      *   Optional IPv4 address (4 bytes)
      * @param ipv6
      *   Optional IPv6 address (16 bytes)
      */
    case SingleHostAddr(port: Option[Int], ipv4: Option[ByteString], ipv6: Option[ByteString])

    /** A relay specified by a DNS name.
      *
      * @param port
      *   Optional port number
      * @param dnsName
      *   The DNS name (A or AAAA record)
      */
    case SingleHostName(port: Option[Int], dnsName: String)

    /** A relay specified by a DNS name for SRV lookup.
      *
      * @param dnsName
      *   The DNS name (SRV record)
      */
    case MultiHostName(dnsName: String)
}

object Relay {

    /** Validates that an IPv4 address is exactly 4 bytes.
      */
    private def validateIpv4(bytes: ByteString): Unit = {
        require(bytes.size == 4, s"IPv4 address must be exactly 4 bytes, got ${bytes.size}")
    }

    /** Validates that an IPv6 address is exactly 16 bytes.
      */
    private def validateIpv6(bytes: ByteString): Unit = {
        require(bytes.size == 16, s"IPv6 address must be exactly 16 bytes, got ${bytes.size}")
    }

    /** Validates that a DNS name is at most 128 characters.
      */
    private def validateDnsName(name: String): Unit = {
        require(name.length <= 128, s"DNS name must be at most 128 characters, got ${name.length}")
    }

    /** CBOR Encoder for Relay. Encodes as a tagged array based on the relay type.
      */
    given Encoder[Relay] = new Encoder[Relay] {
        def write(w: Writer, value: Relay): Writer = value match {
            case Relay.SingleHostAddr(port, ipv4, ipv6) =>
                w.writeArrayOpen(4)
                    .writeInt(0) // Tag for SingleHostAddr

                // Write port or null
                port match {
                    case Some(p) => w.writeInt(p)
                    case None    => w.writeNull()
                }

                // Write IPv4 or null
                ipv4 match {
                    case Some(ip) => w.writeBytes(ip.bytes)
                    case None     => w.writeNull()
                }

                // Write IPv6 or null
                ipv6 match {
                    case Some(ip) => w.writeBytes(ip.bytes)
                    case None     => w.writeNull()
                }

                w.writeArrayClose()

            case Relay.SingleHostName(port, dnsName) =>
                w.writeArrayOpen(3)
                    .writeInt(1) // Tag for SingleHostName

                // Write port or null
                port match {
                    case Some(p) => w.writeInt(p)
                    case None    => w.writeNull()
                }

                // Write DNS name
                w.writeString(dnsName)
                    .writeArrayClose()

            case Relay.MultiHostName(dnsName) =>
                w.writeArrayOpen(2)
                    .writeInt(2) // Tag for MultiHostName
                    .writeString(dnsName)
                    .writeArrayClose()
        }
    }

    /** CBOR Decoder for Relay. Decodes from a tagged array based on the relay type.
      */
    given Decoder[Relay] = new Decoder[Relay] {
        def read(r: Reader): Relay = {
            r.readArrayHeader()
            val tag = r.readInt()

            val relay = tag match {
                case 0 => // SingleHostAddr
                    // Read port
                    val port = if (r.hasNull) {
                        r.readNull()
                        None
                    } else {
                        Some(r.readInt())
                    }

                    // Read IPv4
                    val ipv4 = if (r.hasNull) {
                        r.readNull()
                        None
                    } else {
                        val bytes = ByteString.unsafeFromArray(r.readBytes())
                        validateIpv4(bytes)
                        Some(bytes)
                    }

                    // Read IPv6
                    val ipv6 = if (r.hasNull) {
                        r.readNull()
                        None
                    } else {
                        val bytes = ByteString.unsafeFromArray(r.readBytes())
                        validateIpv6(bytes)
                        Some(bytes)
                    }

                    Relay.SingleHostAddr(port, ipv4, ipv6)

                case 1 => // SingleHostName
                    // Read port
                    val port = if (r.hasNull) {
                        r.readNull()
                        None
                    } else {
                        Some(r.readInt())
                    }

                    // Read DNS name
                    val dnsName = r.readString()
                    validateDnsName(dnsName)

                    Relay.SingleHostName(port, dnsName)

                case 2 => // MultiHostName
                    // Read DNS name
                    val dnsName = r.readString()
                    validateDnsName(dnsName)

                    Relay.MultiHostName(dnsName)

                case _ =>
                    throw new IllegalArgumentException(s"Invalid Relay tag: $tag")
            }

            relay
        }
    }
}
