Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "t1"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Just
            ( Expression'Or
                ( Expression'Or
                    ( Expression'Or
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
                        ( Expression'Equals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "b"
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
                                , value = "b"
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
                            , value = "b"
                            }
                        )
                    )
                    ( Expression'LiteralValue
                        ( Number "8" )
                    )
                )
            )
        , returning = Nothing
        }
    )