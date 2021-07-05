Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "b"
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Negate
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'BitwiseNegate
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Not
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Not
                                    ( Expression'Not
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "b"
                                                }
                                            )
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Minus
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Plus
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Multiply
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Divide
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "b"
                                        }
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
                    , from = Just
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