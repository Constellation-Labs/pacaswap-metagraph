package org.amm_metagraph.l0.custom_routes

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.Encoder
import io.circe.generic.auto._
import org.amm_metagraph.l0.custom_routes.Pagination.PaginationResponse
import org.http4s.EntityEncoder
import org.http4s.circe.jsonEncoderOf

object Responses {

  @derive(encoder, decoder)
  case class PaginatedResponse[A](
    data: A,
    meta: PaginationResponse
  )

  object PaginatedResponse {
    implicit def paginatedResponseEncoder[F[_], A: Encoder]: EntityEncoder[F, PaginatedResponse[A]] =
      jsonEncoderOf[F, PaginatedResponse[A]]
  }

  @derive(encoder, decoder)
  case class SingleResponse[A](
    data: A
  )

  object SingleResponse {
    implicit def singleResponseEncoder[F[_], A: Encoder]: EntityEncoder[F, SingleResponse[A]] =
      jsonEncoderOf[F, SingleResponse[A]]
  }
}
