package me.excel.tools.extractor;


import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Created by hanwen on 16/3/18.
 */
public class BooleanExtractor extends AbstractCellValueExtractor {

  private static final Logger LOGGER = LoggerFactory.getLogger(BooleanExtractor.class);

  public BooleanExtractor(String matchField) {
    super(matchField);
  }

  @Override
  public String getStringValue(Object data, String field) {

    try {
      Object value = PropertyUtils.getProperty(data, getFiledWithOutPrefix(field));

      if (Boolean.TRUE.equals(value)) {
        return "是";
      } else if (Boolean.FALSE.equals(value)) {
        return "否";
      } else {
        return "";
      }
    } catch (NestedNullException e) {
      LOGGER.trace(e.getMessage());
      return "";
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new IllegalArgumentException(e);
    }
  }
}
