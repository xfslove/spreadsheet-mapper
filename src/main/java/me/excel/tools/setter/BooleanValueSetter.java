package me.excel.tools.setter;

import me.excel.tools.BooleanTranslator;
import me.excel.tools.exception.ExcelProcessException;
import me.excel.tools.model.excel.Cell;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static me.excel.tools.FieldUtils.detectRealField;

/**
 * boolean field value setter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class BooleanValueSetter extends FieldValueSetterAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(BooleanValueSetter.class);

  public BooleanValueSetter(String matchField) {
    super(matchField);
  }

  @Override
  public void set(Object data, Cell cell) {
    try {
      BeanUtils.setProperty(data, detectRealField(getMatchField()), BooleanTranslator.parseBoolean(cell.getValue()));
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelProcessException(e);
    }
  }
}
