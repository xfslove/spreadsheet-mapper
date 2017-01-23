package spreadsheet.mapper.w2o.validation.rule.buildin;

import spreadsheet.mapper.w2o.param.BooleanParam;
import spreadsheet.mapper.w2o.validation.rule.DependencyRuleParam;
import spreadsheet.mapper.w2o.validation.rule.SingleCellValidatorFactory;
import spreadsheet.mapper.w2o.validation.validator.cell.buildin.BooleanValidator;

/**
 * Created by hanwen on 2017/1/22.
 */
public class BooleanValidatorFactory implements SingleCellValidatorFactory<BooleanValidator> {

  @Override
  public BooleanValidator create(DependencyRuleParam param, String matchField) {
    Object additionalParam = param.getAdditionalParam();

    if (!(additionalParam instanceof BooleanParam)) {
      throw new IllegalArgumentException("the boolean validator additional param not satisfied, need [BooleanParam]");
    }

    return new BooleanValidator()
        .matchField(matchField)
        .errorMessage(param.getErrorMessage())
        .group(param.getGroup())
        .dependsOn(param.getDependsOn().toArray(new String[0]))
        .param((BooleanParam) additionalParam);
  }

}
