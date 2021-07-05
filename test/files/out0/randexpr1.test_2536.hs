Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Plus
                                ( Expression'Multiply
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
                                    ( Expression'Case
                                        ( CaseExpression
                                            { base = Nothing
                                            , cases =
                                                ( Expression'GreaterThan
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
                                                                            ( Expression'Minus
                                                                                ( Expression'Plus
                                                                                    ( Expression'Plus
                                                                                        ( Expression'Plus
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Just
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "t1"
                                                                                                            }
                                                                                                        )
                                                                                                    , value = "c"
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
                                                                                                                    { expression = Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "d"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    , values =
                                                                                                                        [ Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "a"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        , Expression'Case
                                                                                                                            ( CaseExpression
                                                                                                                                { base = Nothing
                                                                                                                                , cases =
                                                                                                                                    ( Expression'Equals
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
                                                                                                                                                , value = "a"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    , Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "e"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    ) :|
                                                                                                                                    [
                                                                                                                                        ( Expression'Not
                                                                                                                                            ( Expression'Between
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
                                                                                                                                                        , value = "f"
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
                                                                                                                                                        , value = "c"
                                                                                                                                                        }
                                                                                                                                                    )
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
                                                                                                                                                , value = "b"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    ]
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
                                                                                                                        , Expression'Negate
                                                                                                                            ( Expression'LiteralValue
                                                                                                                                ( Number "11" )
                                                                                                                            )
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
                                                                                                        ) :|
                                                                                                        [
                                                                                                            ( Expression'Not
                                                                                                                ( Expression'Exists
                                                                                                                    ( SelectStatement
                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                        , select = CompoundSelect
                                                                                                                            ( SelectCore'Select
                                                                                                                                ( Select
                                                                                                                                    { distinct = False
                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                        ( Aliased
                                                                                                                                            { value = Expression'LiteralValue
                                                                                                                                                ( Number "1" )
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
                                                                                                                                        ( Expression'Or
                                                                                                                                            ( Expression'Not
                                                                                                                                                ( Expression'Between
                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                        ( Number "19" )
                                                                                                                                                    )
                                                                                                                                                    ( Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
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
                                                                                                                                            ( Expression'GreaterThan
                                                                                                                                                ( Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Just
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "t1"
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        , value = "c"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                                ( Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "d"
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
                                                                                                                )
                                                                                                            , Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "d"
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        ]
                                                                                                    , else_ = Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "b"
                                                                                                            }
                                                                                                        )
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
                                                                                    ( Expression'LiteralValue
                                                                                        ( Number "13" )
                                                                                    )
                                                                                )
                                                                                ( Expression'LiteralValue
                                                                                    ( Number "19" )
                                                                                )
                                                                            )
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "d"
                                                                                    }
                                                                                )
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
                                                                                , value = "d"
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
                                                    ( Expression'Negate
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
                                                , Expression'Column
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
                                                ) :|
                                                [
                                                    ( Expression'Or
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
                                                                        , value = "d"
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
                                                                        )
                                                                        ( SelectCore'Select
                                                                            ( Select
                                                                                { distinct = False
                                                                                , columns = ResultColumn'Expression
                                                                                    ( Aliased
                                                                                        { value = Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "c"
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
                                                        )
                                                        ( Expression'Not
                                                            ( Expression'Between
                                                                ( Expression'LiteralValue
                                                                    ( Number "13" )
                                                                )
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
                                                                        , value = "f"
                                                                        }
                                                                    )
                                                                )
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
                                                            , value = "b"
                                                            }
                                                        )
                                                    )
                                                ]
                                            , else_ = Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "e"
                                                    }
                                                )
                                            }
                                        )
                                    )
                                )
                                ( Expression'Negate
                                    ( Expression'Negate
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "a"
                                                }
                                            )
                                        )
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
                    , where_ = Just
                        ( Expression'LessThanOrEquals
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
                            ( Expression'LiteralValue
                                ( Number "11" )
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