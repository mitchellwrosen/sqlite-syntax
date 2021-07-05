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
                        ( Table'InnerJoin
                            ( Table
                                ( QualifiedTableName
                                    { name = Aliased
                                        { value = Namespaced
                                            { namespace = Nothing
                                            , value = "t1"
                                            }
                                        , alias = Nothing
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            )
                            ( Table
                                ( QualifiedTableName
                                    { name = Aliased
                                        { value = Namespaced
                                            { namespace = Nothing
                                            , value = "t2"
                                            }
                                        , alias = Nothing
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            ) Nothing
                        )
                    , where_ = Just
                        ( Expression'Or
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
                                        ( String "A" )
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
                                        ( String "X" )
                                    )
                                )
                            )
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
                                        ( String "A" )
                                    )
                                )
                                ( Expression'Exists
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
                                                                        , value = "t3"
                                                                        }
                                                                    , alias = Nothing
                                                                    }
                                                                , indexedBy = Nothing
                                                                }
                                                            )
                                                        )
                                                    , where_ = Just
                                                        ( Expression'Equals
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "c"
                                                                    }
                                                                )
                                                            )
                                                            ( Expression'LiteralValue
                                                                ( String "C" )
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