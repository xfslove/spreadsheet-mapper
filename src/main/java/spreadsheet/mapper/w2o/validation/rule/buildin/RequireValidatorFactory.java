package spreadsheet.mapper.w2o.validation.rule.buildin;

import spreadsheet.mapper.w2o.validation.rule.DependencyRuleParam;
import spreadsheet.mapper.w2o.validation.rule.SingleCellValidatorFactory;
import spreadsheet.mapper.w2o.validation.validator.cell.buildin.RequireValidator;

/**
 * Created by hanwen on 2017/1/22.
 */
public class RequireValidatorFactory implements SingleCellValidatorFactory<RequireValidator> {

  @Override
  public RequireValidator create(DependencyRuleParam param, String matchField) {
    return new RequireValidator()
        .matchField(matchField)
        .errorMessage(param.getErrorMessage())
        .group(param.getGroup())
        .dependsOn(param.getDependsOn().toArray(new String[0]));
  }
}
