package org.amm_metagraph.shared_data.types.codecs

import io.constellationnetwork.security.Hasher

trait HasherSelector[F[_]] {
  def withBrotli[A](fn: Hasher[F] => A): A
  def withCurrent[A](fn: Hasher[F] => A): A
}

object HasherSelector {
  def apply[F[_]: HasherSelector]: HasherSelector[F] = implicitly
  def forSync[F[_]](hasherBrotli: Hasher[F], hasherCurrent: Hasher[F]): HasherSelector[F] = new HasherSelector[F] {
    def withBrotli[A](fn: Hasher[F] => A): A = fn(hasherBrotli)
    def withCurrent[A](fn: Hasher[F] => A): A = fn(hasherCurrent)
  }
}
