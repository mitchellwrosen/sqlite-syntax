Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Wildcard
                        ( Namespaced
                            { namespace = Nothing
                            , value = ()
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "tA"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'And
                            ( Expression'And
                                ( Expression'And
                                    ( Expression'And
                                        ( Expression'And
                                            ( Expression'And
                                                ( Expression'And
                                                    ( Expression'And
                                                        ( Expression'And
                                                            ( Expression'And
                                                                ( Expression'And
                                                                    ( Expression'And
                                                                        ( Expression'And
                                                                            ( Expression'And
                                                                                ( Expression'And
                                                                                    ( Expression'Equals
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "a"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "1" )
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Equals
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "b"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "2" )
                                                                                        )
                                                                                    )
                                                                                )
                                                                                ( Expression'Equals
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "c"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                    ( Expression'LiteralValue
                                                                                        ( Number "3" )
                                                                                    )
                                                                                )
                                                                            )
                                                                            ( Expression'Equals
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "d"
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( Expression'LiteralValue
                                                                                    ( Number "4" )
                                                                                )
                                                                            )
                                                                        )
                                                                        ( Expression'Equals
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "e"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'LiteralValue
                                                                                ( Number "5" )
                                                                            )
                                                                        )
                                                                    )
                                                                    ( Expression'Equals
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "f"
                                                                                }
                                                                            )
                                                                        )
                                                                        ( Expression'LiteralValue
                                                                            ( Number "6" )
                                                                        )
                                                                    )
                                                                )
                                                                ( Expression'Equals
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "g"
                                                                            }
                                                                        )
                                                                    )
                                                                    ( Expression'LiteralValue
                                                                        ( Number "7" )
                                                                    )
                                                                )
                                                            )
                                                            ( Expression'Equals
                                                                ( Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "h"
                                                                        }
                                                                    )
                                                                )
                                                                ( Expression'LiteralValue
                                                                    ( Number "8" )
                                                                )
                                                            )
                                                        )
                                                        ( Expression'Equals
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "i"
                                                                    }
                                                                )
                                                            )
                                                            ( Expression'LiteralValue
                                                                ( Number "1" )
                                                            )
                                                        )
                                                    )
                                                    ( Expression'Equals
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "j"
                                                                }
                                                            )
                                                        )
                                                        ( Expression'LiteralValue
                                                            ( Number "2" )
                                                        )
                                                    )
                                                )
                                                ( Expression'Equals
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "k"
                                                            }
                                                        )
                                                    )
                                                    ( Expression'LiteralValue
                                                        ( Number "3" )
                                                    )
                                                )
                                            )
                                            ( Expression'Equals
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "l"
                                                        }
                                                    )
                                                )
                                                ( Expression'LiteralValue
                                                    ( Number "4" )
                                                )
                                            )
                                        )
                                        ( Expression'Equals
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "m"
                                                    }
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "5" )
                                            )
                                        )
                                    )
                                    ( Expression'Or
                                        ( Expression'Or
                                            ( Expression'Equals
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "p"
                                                        }
                                                    )
                                                )
                                                ( Expression'LiteralValue
                                                    ( Number "1" )
                                                )
                                            )
                                            ( Expression'Equals
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "p"
                                                        }
                                                    )
                                                )
                                                ( Expression'LiteralValue
                                                    ( Number "2" )
                                                )
                                            )
                                        )
                                        ( Expression'Equals
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "p"
                                                    }
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "3" )
                                            )
                                        )
                                    )
                                )
                                ( Expression'Equals
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "n"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "6" )
                                    )
                                )
                            )
                            ( Expression'Equals
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "o"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "7" )
                                )
                            )
                        )
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )