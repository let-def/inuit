open Inuit_base

(** Region is the basic entity through which the buffer is manipulated.

    It allows to change the text in an abstract way.
    Physical positions (offsets) don't need to be exposed, hence the region
    is made more robust agains concurrent changes.

    Furthermore, a region can contain arbitrarily many sub-regions.
    Sub-regions cannot overlap, such that sub-regions and their sub-regions
    form a tree which is mapped on the actual text of the buffer.
*)
type 'flags t

(** Add text to the right end of the region. The meaning of the [flags] is
    defined by the backend (consumer of the region). *)
val append : 'flags t -> 'flags list -> string -> unit

(** Erase the content of the region.
    All sub-regions are [kill]ed (erased and closed). See [sub] to learn more
    about sub-regions.

    New content can be added again through [append].  The most basic way to
    introduce interactivity is by replacing the content of a region. *)
val clear : 'flags t -> unit

(** Erase the content and close the region, see below. *)
val kill : 'flags t -> unit

(** Apply some flags on the region *)
val propertize : 'flags list -> 'flags t -> unit

(** One can put an observer to listen to patches applied on a region.
    This is the lowest level way to get feedback from actions done by the
    consumer of the buffer.

    The observer will get the region the patch applies to, the side which
    produced the patch and the patch itself.

    When the observer is invoked, the buffer has not yet been modified by the
    patch, and it is incorrect to change the buffer inside the callback.

    If you want to make changes to the buffer, you can return another function
    which will be invoked as soon as the all pending patches are applied.

    [offset] of the patch is absolute. You can use the [unsafe_left_offset]
    and [unsafe_right_offset] functions below to get the physical position of
    the region to see which part is affected by the patch.

    [patch] can overlap the region if text deleted is larger than the region
    itself. *)
type 'flags observer =
  'flags t -> side -> 'flags Patch.t ->
  'flags list * (unit -> unit) option

(** [`Local] changes are the one made through the [Inuit_region] and
    [Inuit_cursor] API.
    [`Remote] changes are the one received through the socket. *)
and side = [ `Local | `Remote ]

(** Create a sub-region at the right end of the region.
    The new region is an empty.
    The effect of this function is to turn
         [... region1 ... text ...]
    into [... region1 ... text ...[region2]], where [region2] is empty. *)
val sub : ?at:[`Left | `Right] ->
  ?observer:'flags observer -> 'flags t -> 'flags t

(** A region is closed if it no longer has any listener, such that changes
    applied to it will never be observed again.

    You can for instance use this to skip expensive computations to produce
    text that will anyway never be useful. *)
val is_closed : 'flags t -> bool

(** Get the physical position of the region of the left end of the region.

    It is unsafe because physical positions refers to the current version of
    the buffer and might get invalidated by any change.

    Also, result of [unsafe_left_offset] is not specified on a closed region.
*)
val unsafe_left_offset  : 'flags t -> int

(** Get the physical position of the region of the right end of the region.
    The remarks of [unsafe_left_offset] also apply here.  *)
val unsafe_right_offset : 'flags t -> int

(** {1 Constructing regions} *)

(** Return a new root region and the socket to which the patches will be
    streamed.
    The socket should be connected to a consumer before any changes is done. *)
val make : unit -> 'flags t * 'flags patch socket

(** A dummy region that is always closed. Useful as a placeholder. *)
val null : _ t
