Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = True
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Column
                                ( Namespaced
                                    { namespace = Just
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    , value = "val"
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Case
                                    ( CaseExpression
                                        { base = Nothing
                                        , cases =
                                            ( Expression'Equals
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Just
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "b"
                                                                }
                                                            )
                                                        , value = "val"
                                                        }
                                                    )
                                                )
                                                ( Expression'LiteralValue
                                                    ( String "1" )
                                                )
                                            , Expression'LiteralValue
                                                ( String "xyz" )
                                            ) :| []
                                        , else_ = Expression'Column
                                            ( Namespaced
                                                { namespace = Just
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "b"
                                                        }
                                                    )
                                                , value = "val"
                                                }
                                            )
                                        }
                                    )
                                , alias = Just "col1"
                                }
                            )
                        ]
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "b"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Nothing
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )