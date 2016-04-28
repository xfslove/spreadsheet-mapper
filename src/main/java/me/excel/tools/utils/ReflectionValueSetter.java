package me.excel.tools.utils;

import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.ConvertUtils;
import org.apache.commons.beanutils.converters.*;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.List;

import static me.excel.tools.utils.BooleanTranslator.parseBoolean;
import static me.excel.tools.FieldUtils.getFieldType;
import static me.excel.tools.FieldUtils.getFieldWithoutPrefix;


/**
 * reflection object value setter
 *
 * Created by hanwen on 15-12-18.
 */
public class ReflectionValueSetter {

  private static final Logger LOGGER = LoggerFactory.getLogger(ReflectionValueSetter.class);

  static {
    ConvertUtils.register(new DateConverter(null), java.util.Date.class);
    ConvertUtils.register(new CalendarConverter(null), Calendar.class);
    ConvertUtils.register(new SqlDateConverter(null), java.sql.Date.class);
    ConvertUtils.register(new SqlTimeConverter(null), Time.class);
    ConvertUtils.register(new SqlTimestampConverter(null), Timestamp.class);
    ConvertUtils.register(new BigDecimalConverter(null), BigDecimal.class);
  }

  public void set(Object data, List<ExcelCell> excelCells) {

    excelCells.forEach(cell -> {
      try {
        BeanUtils.setProperty(data, getFieldWithoutPrefix(cell.getField()), convertValue(data.getClass(), cell));
      } catch (Exception e) {
        LOGGER.error(ExceptionUtils.getStackTrace(e));
        throw new RuntimeException(e);
      }
    });

  }



  private Object convertValue(Class targetClazz, ExcelCell cell) {
    String value = cell.getValue();
    if (value == null) {
      return null;
    }
    Class declaredFieldType = getFieldType(targetClazz, getFieldWithoutPrefix(cell.getField()).split("\\."));
    if (declaredFieldType == null) {
      return null;
    }
    if (String.class.isAssignableFrom(declaredFieldType)) {
      return value;
    } else if (Integer.class.isAssignableFrom(declaredFieldType)
        || int.class.isAssignableFrom(declaredFieldType)) {
      return Integer.parseInt(value);
    } else if (Double.class.isAssignableFrom(declaredFieldType)
        || double.class.isAssignableFrom(declaredFieldType)) {
      return Double.parseDouble(value);
    } else if (Boolean.class.isAssignableFrom(declaredFieldType)
        || boolean.class.isAssignableFrom(declaredFieldType)) {
      return parseBoolean(value);
    }
    return null;
  }

}
