package me.excel.tools.setter;

import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static me.excel.tools.FieldUtils.getFieldWithoutPrefix;

/**
 * Created by hanwen on 5/3/16.
 */
public class IntValueSetter extends AbstractCellValueSetter {

  private static final Logger LOGGER = LoggerFactory.getLogger(FloatValueSetter.class);

  public IntValueSetter(String matchField) {
    super(matchField);
  }

  @Override
  public void set(Object data, ExcelCell excelCell) {
    try {
      BeanUtils.setProperty(data, getFieldWithoutPrefix(excelCell.getField()), Integer.parseInt(excelCell.getValue()));
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new RuntimeException(e);
    }
  }
}
