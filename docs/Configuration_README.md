# Forsys.R Configuration File README

| Parameter name | Input | Input type (required) | Example |
| -------------- | ----- | --------------------- | ------- |
| scenario_name | Unique scenario name. This will be used to save outputs. | String (required)| "Idaho_test"|
| is_dbf | Defines if input stand file is a dbf file. | Logical (required) | TRUE or FALSE |
| is_csv | Defines if input stand file is a csv file. | Logical (required) | TRUE or FALSE |
| input_standfile | Pathway and name to stand layer file | CSV file pathway as string (required) | "data/IDHexnet_North20190523_Final.dbf" |
| write_stand_outputs  | Flag to turn stand outputs on or off | Logical (required) | TRUE of FALSE |
| pcp_spm  | Variables that must have the percent contribution to the problem (PCP) and the standardized percent of the max (SPM) computed. Must include the priority variable(s). Equivalent to Calculate PCP and SPM in ForSysX. | List of field names as string (required) | c("HUSUM_STND", "TVMBF_STND") |
| land_base | The basis for PCP and SPM calculations. Must be binary field. Equivalent to Available for Management in ForSysX. | Field name as string (required) | “man_alldis” or set to NULL |
| stand_field | Field that contains the unique ID for each stand. Note this is Stand ID in ForSysX, not to be confused with Subunit in ForSysX.  | Field name as string (required) | "Cell_ID" |
| priorities | Priority values | List of field names as string (required) | c("HUSUM_SPM", "TVMBF_SPM") |
| stand_group_by | Planning area ID. Each stand belongs to one planning unit. Equivalent to Subunit in ForSysX. | Field name as string (required) | "PA_ID" |
| pa_target  | This is the variable target treatment for each planning unit. It may be an area-based metric (i.e. treat 30% of the area in each planning unit), or a value-based metric (i.e. treat 30% of the total building exposure in each planning unit). This value allows ForSys to vary the target by planning unit. | Field name as string (required) | "AREA_MAN" |
| pa_unit | This is the field that counts towards the treatment target when a stand is treated. | Field name as string (required) | "AREA_HA" |
| pa_target_multiplier | The multiplier can be used to calculate a proportion of the target, so the user does not have to recalculate the pa_target variable each time they want to increase or decrease it. | Decimal value between 0 and 1. (required) | 0.15 |
| nesting | Defines if nesting will be included. This allows the user to select planning areas within a larger container. Equivalent to Master Subunit in ForSysX. | Logical (required) | TRUE or FALSE |
| nesting_group_by | Nesting identifier, such as national forest ID. Equivalent to Master Subunit in ForSysX. | Field name as string (required for nesting == TRUE, otherwise NULL) | “FORESTID” |
| nesting_target | This is the variable treatment target for the larger planning areas. It may be an attainment target (i.e. harvest volume by national forest), or a cost constraint (spend a budget on fuel management in each fireshed). | Field name as string (required for nesting == TRUE, otherwise NULL) | “FOREST_HA” or NULL |
| nesting_unit | This is the field that counts towards the treatment target when a planning area is treated. | Field name as string (required for nesting == TRUE, otherwise NULL) | "AREA_HA" |
| nesting_target_multiplier  | The multiplier can be used to calculate a percentage of the target, so the user does not have to recalculate the nesting_target each time they want to increase or decrease it. | Decimal value between 0 and 1. (required for nesting == TRUE, otherwise NULL) | 0.15 |
| weighting_values | Defines the weights and integer steps between weights. The values are for min, max, and step. | List of three integers. (required) | c("0 5 1") |
| thresholds  | Thresholds are defined first by treatment type (the first value in the string) and can be used to incorporate multiple treatment types. The current code only uses one type (Manageable). The ‘Manageable’ string is a required setting. | List of strings with treatment type and threshold values. Note the unusual syntax without a comma separating the two values. (required) | c("Manageable man_alldis ==1", “Precommercial TVMBF_STND > 1”) |
| include_stands | This defines global threshold values to include stands - i.e. for any threshold type. | List of strings with threshold values. Inclusion criteria are "and", not "or". (can be NULL)  | c("Treatable_HA > 0", "HUSUM_STND > 0") |
| output_fields  | A list of the desired fields for the planning area treatment files. Planning area id, priority weights and treatment rank are added automatically. | List of field names as strings. (required) | c("AREA_HA", "TVMBF_STND", "TVMBF_PCP", "HUSUM_STND", "HUSUM_PCP") |
| grouping_variables  | Any non-standard groupings, such as ownership or watershed. | List of fields as strings. (Minimum of one required – subunit_group_by) | c("PA_ID", "Owner") |
| fixed_target  | Set to have either a fixed planning unit target (TRUE) or a variable planning unit target (FALSE). An example would be a specific target area for each planning unit vs. a 2000 ha area target for every planning unit. The equivalent for FALSE in ForSysX is ‘Constraint by Subunit’. | Logical (required) | TRUE or FALSE |
| fixed_target_value | Set to the target value that will be used for every subunit when fixed_target == TRUE | Number | 2000 |