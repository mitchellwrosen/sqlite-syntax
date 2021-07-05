Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'FunctionCall
                                ( FunctionCallExpression
                                    { call = FunctionCall
                                        { name = Namespaced
                                            { namespace = Nothing
                                            , value = "coalesce"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'Subquery
                                                ( SelectStatement
                                                    { commonTableExpressions = Nothing
                                                    , select = CompoundSelect
                                                        ( SelectCore'Select
                                                            ( Select
                                                                { distinct = False
                                                                , columns = ResultColumn'Expression
                                                                    ( Aliased
                                                                        { value = Expression'FunctionCall
                                                                            ( FunctionCallExpression
                                                                                { call = FunctionCall
                                                                                    { name = Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "max"
                                                                                        }
                                                                                    , arguments = FunctionArguments'Arguments
                                                                                        [ Expression'Multiply
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Just
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "t1"
                                                                                                            }
                                                                                                        )
                                                                                                    , value = "d"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                            ( Expression'Case
                                                                                                ( CaseExpression
                                                                                                    { base = Nothing
                                                                                                    , cases =
                                                                                                        ( Expression'Not
                                                                                                            ( Expression'InValues
                                                                                                                ( InValuesExpression
                                                                                                                    { expression = Expression'Case
                                                                                                                        ( CaseExpression
                                                                                                                            { base = Nothing
                                                                                                                            , cases =
                                                                                                                                ( Expression'InSubquery
                                                                                                                                    ( InSubqueryExpression
                                                                                                                                        { expression = Expression'Minus
                                                                                                                                            ( Expression'Plus
                                                                                                                                                ( Expression'Plus
                                                                                                                                                    ( Expression'Minus
                                                                                                                                                        ( Expression'Minus
                                                                                                                                                            ( Expression'Plus
                                                                                                                                                                ( Expression'BitwiseNegate
                                                                                                                                                                    ( Expression'Negate
                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                { namespace = Just
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                        , value = "t1"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                , value = "d"
                                                                                                                                                                                }
                                                                                                                                                                            )
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                    ( Number "17" )
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "b"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                        ( Expression'Multiply
                                                                                                                                                            ( Expression'Multiply
                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                    ( Number "13" )
                                                                                                                                                                )
                                                                                                                                                                ( Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Just
                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                }
                                                                                                                                                                            )
                                                                                                                                                                        , value = "f"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                            ( Expression'Divide
                                                                                                                                                                ( Expression'FunctionCall
                                                                                                                                                                    ( FunctionCallExpression
                                                                                                                                                                        { call = FunctionCall
                                                                                                                                                                            { name = Namespaced
                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                , value = "abs"
                                                                                                                                                                                }
                                                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                                                [ Expression'Plus
                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            , value = "b"
                                                                                                                                                                                            }
                                                                                                                                                                                        )
                                                                                                                                                                                    )
                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                                    )
                                                                                                                                                                                ]
                                                                                                                                                                            }
                                                                                                                                                                        , filter = Nothing
                                                                                                                                                                        , over = Nothing
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                                ( Expression'FunctionCall
                                                                                                                                                                    ( FunctionCallExpression
                                                                                                                                                                        { call = FunctionCall
                                                                                                                                                                            { name = Namespaced
                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                , value = "abs"
                                                                                                                                                                                }
                                                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                                                [ Expression'Column
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        , value = "a"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                ]
                                                                                                                                                                            }
                                                                                                                                                                        , filter = Nothing
                                                                                                                                                                        , over = Nothing
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                    ( Expression'Multiply
                                                                                                                                                        ( Expression'Multiply
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "a"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Just
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "t1"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    , value = "e"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                        ( Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Just
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "t1"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                , value = "a"
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                                ( Expression'Multiply
                                                                                                                                                    ( Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "c"
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                    ( Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Just
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "t1"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            , value = "d"
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                            ( Expression'Column
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Just
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "t1"
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    , value = "a"
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                        , subquery = SelectStatement
                                                                                                                                            { commonTableExpressions = Nothing
                                                                                                                                            , select = Union
                                                                                                                                                ( CompoundSelect
                                                                                                                                                    ( SelectCore'Select
                                                                                                                                                        ( Select
                                                                                                                                                            { distinct = False
                                                                                                                                                            , columns = ResultColumn'Expression
                                                                                                                                                                ( Aliased
                                                                                                                                                                    { value = Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            , value = "b"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    , alias = Nothing
                                                                                                                                                                    }
                                                                                                                                                                ) :| []
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
                                                                                                                                                )
                                                                                                                                                ( SelectCore'Select
                                                                                                                                                    ( Select
                                                                                                                                                        { distinct = False
                                                                                                                                                        , columns = ResultColumn'Expression
                                                                                                                                                            ( Aliased
                                                                                                                                                                { value = Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Just
                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                }
                                                                                                                                                                            )
                                                                                                                                                                        , value = "d"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                , alias = Nothing
                                                                                                                                                                }
                                                                                                                                                            ) :| []
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
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Just
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "t1"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        , value = "e"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                ) :| []
                                                                                                                            , else_ = Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Just
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "t1"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    , value = "e"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            }
                                                                                                                        )
                                                                                                                    , values =
                                                                                                                        [ Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Just
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "t1"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , value = "f"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        , Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Just
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "t1"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , value = "e"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        , Expression'LiteralValue
                                                                                                                            ( Number "19" )
                                                                                                                        ]
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        , Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Just
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "t1"
                                                                                                                        }
                                                                                                                    )
                                                                                                                , value = "d"
                                                                                                                }
                                                                                                            )
                                                                                                        ) :| []
                                                                                                    , else_ = Expression'LiteralValue
                                                                                                        ( Number "11" )
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        ]
                                                                                    }
                                                                                , filter = Nothing
                                                                                , over = Nothing
                                                                                }
                                                                            )
                                                                        , alias = Nothing
                                                                        }
                                                                    ) :| []
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
                                                                , where_ = Just
                                                                    ( Expression'Not
                                                                        ( Expression'Between
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "d"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Just
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "t1"
                                                                                            }
                                                                                        )
                                                                                    , value = "a"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "a"
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
                                            , Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "c"
                                                    }
                                                )
                                            ]
                                        }
                                    , filter = Nothing
                                    , over = Nothing
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :| []
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
                    , where_ = Just
                        ( Expression'Not
                            ( Expression'InSubquery
                                ( InSubqueryExpression
                                    { expression = Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "t1"
                                                    }
                                                )
                                            , value = "e"
                                            }
                                        )
                                    , subquery = SelectStatement
                                        { commonTableExpressions = Nothing
                                        , select = Union
                                            ( CompoundSelect
                                                ( SelectCore'Select
                                                    ( Select
                                                        { distinct = False
                                                        , columns = ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "a"
                                                                        }
                                                                    )
                                                                , alias = Nothing
                                                                }
                                                            ) :| []
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
                                            )
                                            ( SelectCore'Select
                                                ( Select
                                                    { distinct = False
                                                    , columns = ResultColumn'Expression
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
                                                                        { namespace = Just
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "t1"
                                                                                }
                                                                            )
                                                                        , value = "b"
                                                                        }
                                                                    )
                                                                )
                                                            , alias = Nothing
                                                            }
                                                        ) :| []
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
                                    }
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