package org.amm_metagraph.shared_data

import cats.effect.Async
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof


object Utils {
  def toAddress[F[_] : Async : SecurityProvider](proof: SignatureProof): F[Address] =
    proof.id.toAddress
}