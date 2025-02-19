package org.amm_metagraph.l0.custom_routes

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.generic.auto._

object Pagination {

  @derive(encoder, decoder)
  case class PaginationParams(
    limit: Int = 10,
    offset: Int = 0
  )

  object PaginationParams {
    def fromStrings(maybeLimit: Option[String], maybeOffset: Option[String]): PaginationParams =
      PaginationParams(
        limit = maybeLimit.flatMap(_.toIntOption).getOrElse(10),
        offset = maybeOffset.flatMap(_.toIntOption).getOrElse(0)
      )
  }

  @derive(encoder, decoder)
  case class PaginationResponse(
    total: Int,
    limit: Int,
    offset: Int,
    has_more: Boolean
  )

}
