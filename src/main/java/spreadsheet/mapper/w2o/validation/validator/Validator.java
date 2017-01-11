package spreadsheet.mapper.w2o.validation.validator;

/**
 * excel meta validator
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public interface Validator {

  /**
   * the error message will be collected when validator failure if error message is not blank
   *
   * @return valid error message
   */
  String getErrorMessage();
}
