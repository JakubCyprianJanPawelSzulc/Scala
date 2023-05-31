package jp1.akka.lab13.model

// Generator osób i ocen
object Utl {
  import java.time.Instant
  import java.util.Locale
  import org.scalacheck.rng.Seed
  import faker._

  def osoba(): Osoba = {
    val firstName: String = Faker.en_US.firstName()
    val lastName: String = Faker.en_US.lastName()
    Osoba(firstName, lastName)
  }

  import scala.util.Random
  val rand = new Random
  val próbaUdana = 0.05

  def ocena(): Option[Ocena] = {
    if (rand.nextDouble() > próbaUdana) {
      val nota1 = rand.nextInt(21)
      val nota2 = rand.nextInt(21)
      val nota3 = rand.nextInt(21)
      Some(Ocena(nota1, nota2, nota3))
    } else {
      None
    }
  }

}

